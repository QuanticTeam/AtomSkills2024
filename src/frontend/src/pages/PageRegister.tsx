import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Alert, App, Button, Form, Input, Popover, Select, Space, Typography } from 'antd'
import { useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { Link, useNavigate } from 'react-router-dom'
import { isStrongPassword } from 'validator'
import { z } from 'zod'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { apiClient, getIsDetailedApiError } from '~/shared/apiClient'
import { Logo } from '~/shared/ui'

function getIsPasswordStrong(value: string, returnScore = false): boolean | number {
  return isStrongPassword(value, {
    // TODO configure
    minLength: 8,
    minLowercase: 1,
    minUppercase: 1,
    minNumbers: 1,
    minSymbols: 1,
    returnScore: returnScore as any,
    pointsPerUnique: 1,
    pointsPerRepeat: 0.5,
    pointsForContainingLower: 20,
    pointsForContainingUpper: 20,
    pointsForContainingNumber: 20,
    pointsForContainingSymbol: 20,
  })
}

enum UserRole {
  User,
  Expert,
  Admin,
}

const schema = z.object({
  login: z
    .string()
    .trim()
    .min(1, ' ')
    .min(1, 'Слишком короткое имя пользователя')
    .max(56, 'Слишком длинное имя пользователя')
    // TODO validate latins
    .refine(async login => {
      return !(await apiClient.post('/User/CheckLogin', { login })).data.taken
    }, 'Логин уже занят'),
  password: z
    .string()
    .trim()
    .min(1, ' ')
    .min(8, 'Слишком короткий пароль')
    .max(32, 'Слишком длинный пароль')
    .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, 'Недопустимые символы'),
  // .refine(p => !getIsPasswordStrong(p), 'Пароль слишком слабый'), // TODO
  role: z.nativeEnum(UserRole),
})

type Data = z.infer<typeof schema>

export default function PageRegister() {
  const navigate = useNavigate()
  const { notification } = App.useApp()
  const [submissionError, setSubmissionError] = useState('')

  const {
    handleSubmit,
    control,
    formState: { errors },
    setError,
  } = useForm<Data>({
    mode: 'onBlur',
    values: {
      login: '',
      password: '',
      role: UserRole.User, // TODO empty by default
    },
    resolver: zodResolver(schema, { async: true }),
  })

  const onSubmit: SubmitHandler<Data> = async data => {
    try {
      await apiClient.post('/User/Register', data)
      notification.info({
        message: 'Вы успешно зарегистрированы',
        description: 'Используйте учетные данные для входа в систему',
        duration: 10,
      })
      navigate('/login')
    } catch (error) {
      if (getIsDetailedApiError(error)) {
        if (error.code === 'REGISTER_USERNAME_TAKEN') {
          setError('login', { message: 'Логин уже занят' }, { shouldFocus: true })
          return
        }

        setSubmissionError(error.code)
        return
      }

      throw error
    }
  }

  return (
    <PageUnauthorized>
      <div className="w-72 flex mx-auto mb-16">
        <Logo full />
      </div>

      <div className="flex">
        <Form
          className="w-4/5 m-auto"
          labelCol={{ span: 3, offset: 5 }}
          wrapperCol={{ span: 8 }}
          layout="horizontal"
          onFinish={handleSubmit(onSubmit)}
        >
          <Controller
            name="role"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label="Ваша роль"
                help={errors.role?.message}
                validateStatus={errors.role?.message && 'error'}
              >
                <Select
                  options={[
                    {
                      value: UserRole.User,
                      label: 'Участник',
                    },
                    {
                      value: UserRole.Expert,
                      label: 'Эксперт',
                    },
                    {
                      value: UserRole.Admin,
                      label: 'Администратор',
                    },
                  ]}
                  placeholder="Выбрать"
                  {...field}
                />
              </Form.Item>
            )}
          />
          <Controller
            name="login"
            control={control}
            render={({ field }) => (
              <Form.Item
                label="Пользователь"
                required
                help={errors.login?.message}
                validateStatus={errors.login?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Латинские буквы и цифры от 2 до 56 символов
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      prefix={<UserOutlined className="text-slate-400" />}
                      placeholder="Придумайте логин"
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />
          <Controller
            name="password"
            control={control}
            render={({ field }) => (
              <Form.Item
                label="Пароль"
                required
                help={errors.password?.message}
                validateStatus={errors.password?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Латинские буквы, цифры,{' '}
                          <code
                            className="font-bold text-slate-700"
                            title="- _ + ! ? = # $ % & @ ^ ` ~"
                          >
                            - _ + ! ? = # $ % & @ ^ ` ~
                          </code>{' '}
                          от 8 до 32 символов
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input.Password
                      prefix={<LockOutlined className="text-slate-400" />}
                      placeholder="Придумайте пароль"
                      suffix={
                        <Typography.Text className="text-xs text-slate-400">
                          {getIsPasswordStrong(field.value) ? 'надежный' : 'слабый'}
                        </Typography.Text>
                      }
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />

          <Form.Item
            label=" "
            colon={false}
          >
            <Space
              className="w-full"
              direction="vertical"
              classNames={{ item: 'flex justify-center' }}
            >
              <Button
                type="primary"
                htmlType="submit"
                className="w-full"
              >
                Зарегистрироваться
              </Button>

              {!submissionError ? null : (
                <Typography.Text type="danger">{submissionError}</Typography.Text>
              )}

              <Typography.Text>
                или <Link to="/login">войти</Link>
              </Typography.Text>
            </Space>
          </Form.Item>
        </Form>
      </div>
    </PageUnauthorized>
  )
}
