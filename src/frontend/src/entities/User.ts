import { z } from 'zod'
import { apiClient } from '~/shared/apiClient'

export const UserApi = {
  async register(dto: UserRegisterDto) {
    return await apiClient.post('/User/Register', dto)
  },
  async login(dto: UserLoginDto) {
    return (await apiClient.post('/User/Login', dto)).data
  },
}

export const userRegisterSchema = z.object({
  login: z
    .string()
    .trim()
    .min(1, ' ')
    .min(1, 'Слишком короткое имя пользователя') // TODO t
    .max(56, 'Слишком длинное имя пользователя') // TODO t
    // TODO validate latins
    .refine(async login => {
      return !(await apiClient.post('/User/CheckLogin', { login })).data.taken
    }, 'Логин уже занят'), // TODO t
  password: z
    .string()
    .trim()
    .min(1, ' ')
    .min(8, 'Слишком короткий пароль') // TODO t
    .max(32, 'Слишком длинный пароль') // TODO t
    .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, 'Недопустимые символы'),
  // .refine(p => !getIsPasswordStrong(p), 'Пароль слишком слабый'), // TODO && TODO t
  role: z.number(),
})

export type UserRegisterDto = z.infer<typeof userRegisterSchema>

export const userLoginSchema = z.object({
  login: z.string().trim().min(1, ' '), // TODO investigate
  password: z.string().trim().min(1, ' '),
  remember: z.boolean(),
})

export type UserLoginDto = z.infer<typeof userLoginSchema>
