import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Alert, Button, Form, Input, Popover, Select, Space, Typography } from 'antd'
import { ReactNode, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { getIsPasswordStrong } from '~/entities/Password'
import { UserRegisterDto, userRegisterSchema } from '~/entities/User'
import { UserRole } from '~/entities/UserRole'
import { getIsDetailedApiError } from '~/shared/apiClient'

interface FormRegisterProps {
  children: ReactNode
  onSubmit: SubmitHandler<UserRegisterDto>
  roles: UserRole[]
}

export function FormRegister({ children, onSubmit, roles }: FormRegisterProps) {
  const {
    handleSubmit,
    control,
    formState: { errors },
    setError,
  } = useForm<UserRegisterDto>({
    mode: 'onBlur',
    values: {
      login: '',
      password: '',
      role: 0, // TODO empty by default
    },
    resolver: zodResolver(userRegisterSchema, { async: true }),
  })

  const [submissionError, setSubmissionError] = useState('')

  return (
    <Form
      className="w-4/5 m-auto"
      labelCol={{ span: 3, offset: 5 }}
      wrapperCol={{ span: 8 }}
      layout="horizontal"
      onFinish={async (...args) => {
        try {
          await handleSubmit(onSubmit)(...args)
        } catch (error) {
          if (!getIsDetailedApiError(error)) throw error

          switch (error.code) {
            case 'REGISTER_USERNAME_TAKEN': {
              setError('login', { message: 'Логин уже занят' }, { shouldFocus: true }) // TODO t
              return
            }
            default: {
              setSubmissionError(error.code)
            }
          }
        }
      }}
    >
      <Controller
        name="role"
        control={control}
        render={({ field }) => (
          <Form.Item
            required
            label="Ваша роль" // TODO t
            help={errors.role?.message}
            validateStatus={errors.role?.message && 'error'}
          >
            <Select
              options={roles.map(({ id, role }) => ({ value: id, label: role }))}
              placeholder="Выбрать" // TODO t
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
            label="Пользователь" // TODO t
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
                      {/* // TODO t */}
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
                  placeholder="Придумайте логин" // TODO t
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
            label="Пароль" // TODO t
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
                      {/* TODO t */}
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
                  placeholder="Придумайте пароль" // TODO t
                  suffix={
                    <Typography.Text className="text-xs text-slate-400">
                      {/* TODO t */}
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
            {/* TODO t */}
            Зарегистрироваться
          </Button>

          {!submissionError ? null : (
            <Typography.Text type="danger">{submissionError}</Typography.Text>
          )}

          {children}
        </Space>
      </Form.Item>
    </Form>
  )
}
