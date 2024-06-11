import { t } from 'i18next'
import { z } from 'zod'
import { apiClient } from '~/shared/apiClient'

export const UserApi = {
  async signUp(dto: UserSignUpDto) {
    return await apiClient.post('/User/SignUp', dto)
  },
  async signIn(dto: UserSignInDto) {
    return (await apiClient.post('/User/SignIn', dto)).data
  },
}

export const userSignUpSchema = z.object({
  login: z
    .string()
    .trim()
    .min(1, ' ')
    .min(1, t('Login is too short'))
    .max(56, t('Login is too long'))
    // TODO validate latins
    .refine(async login => {
      return !(await apiClient.post('/User/CheckLogin', { login })).data.taken
    }, t('Login is taken')),
  password: z
    .string()
    .trim()
    .min(1, ' ')
    .min(8, t('Password is too short'))
    .max(32, t('Password is too long'))
    .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, t('Unacceptable symbols')),
  // .refine(p => !getIsPasswordStrong(p), t('Password is too weak'))), // TODO
  role: z.number(),
})

export type UserSignUpDto = z.infer<typeof userSignUpSchema>

export const userSignInSchema = z.object({
  login: z.string().trim().min(1, ' '), // TODO investigate
  password: z.string().trim().min(1, ' '),
  remember: z.boolean(),
})

export type UserSignInDto = z.infer<typeof userSignInSchema>
